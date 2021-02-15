/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngineAsserts.assertUntilCommunityUserStateIs;
import static eu.internetofus.common.components.profile_manager.WeNetProfileManagers.createUsers;
import static eu.internetofus.common.components.service.WeNetServiceSimulators.createApp;
import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;
import static eu.internetofus.common.components.task_manager.WeNetTaskManagers.waitUntilTask;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.HumanDescription;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.Message;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the hard coded protocol. ATTENTION: This test is
 * sequential and maintains the state between methods. In other words, you must
 * to run the entire test methods on the specified order to work.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
@TestMethodOrder(OrderAnnotation.class)
public class HardCodedQuestionAndAnswersProtocolIT {

  /**
   * Number maximum of users to use on the test.
   */
  private static final int MAX_USERS = 6;

  /**
   * The users that will involved on the test.
   */
  protected static List<WeNetUserProfile> users;

  /**
   * The application that will involved on the test.
   */
  protected static App app;

  /**
   * The task that will involved on the test.
   */
  protected static Task task;

  /**
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertComplete(createUsers(MAX_USERS, vertx, testContext)).onSuccess(users -> {
      HardCodedQuestionAndAnswersProtocolIT.users = users;
      testContext.completeNow();
    });

  }

  /**
   * Create the app that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(2)
  public void shouldCreateApp(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.users != null;
    testContext.assertComplete(createApp(HardCodedQuestionAndAnswersProtocolIT.users, vertx, testContext))
        .onSuccess(app -> {
          HardCodedQuestionAndAnswersProtocolIT.app = app;
          testContext.completeNow();
        });

  }

  /**
   * Check that a task is created.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(3)
  public void shouldCreateTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.users != null;
    assert HardCodedQuestionAndAnswersProtocolIT.app != null;

    final var task = new Task();
    task.appId = HardCodedQuestionAndAnswersProtocolIT.app.appId;
    task.attributes = new JsonObject().put("kindOfAnswerer", "anyone");
    task.taskTypeId = WeNetTaskManager.QUESTION_AND_ANSWER_TASK_TYPE_ID;
    task.goal = new HumanDescription();
    task.goal.name = "Test create task";
    task.requesterId = HardCodedQuestionAndAnswersProtocolIT.users.get(0).id;
    final var newState = new State();
    newState.attributes = new JsonObject().put("incentives", new JsonObject().put("Answers", 3));
    final var future = WeNetTaskManager.createProxy(vertx).createTask(task).compose(createdTask -> {
      HardCodedQuestionAndAnswersProtocolIT.task = createdTask;
      return waitUntilCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

        final Set<String> ids = new HashSet<>();
        for (final WeNetUserProfile profile : HardCodedQuestionAndAnswersProtocolIT.users) {

          ids.add(profile.id);
        }
        ids.remove(createdTask.requesterId);

        for (var i = 0; i < callbacks.size(); i++) {

          final var msg = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
          if (msg != null && "QuestionToAnswerMessage".equals(msg.label)
              && createdTask.id.equals(msg.attributes.getString("taskId"))
              && createdTask.goal.name.equals(msg.attributes.getString("question"))
              && createdTask.requesterId.equals(msg.attributes.getString("userId"))) {

            ids.remove(msg.receiverId);

          }
        }
        return ids.isEmpty();

      }, vertx, testContext);

    }).compose(msg -> WeNetServiceSimulator.createProxy(vertx)
        .deleteCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, updatedTask -> {

          return updatedTask.transactions != null && updatedTask.transactions.size() == 1
              && updatedTask.transactions.get(0).messages != null && updatedTask.transactions.get(0).messages
                  .size() == HardCodedQuestionAndAnswersProtocolIT.users.size() - 1;

        }, vertx, testContext)).compose(storedTask -> {

          HardCodedQuestionAndAnswersProtocolIT.task = storedTask;
          testContext.verify(() -> {

            assertThat(storedTask.transactions).isNotEmpty();
            final var transaction = storedTask.transactions.get(0);
            assertThat(transaction.messages).isNotEmpty();
            final var messages = new ArrayList<>(transaction.messages);
            for (final WeNetUserProfile profile : HardCodedQuestionAndAnswersProtocolIT.users) {

              final var iter = messages.iterator();
              while (iter.hasNext()) {

                final var message = iter.next();
                if (profile.id.equals(message.receiverId)) {

                  iter.remove();
                  break;
                }

              }
            }

            assertThat(messages).isEmpty();
          });

          return Future.succeededFuture(storedTask);

        }).compose(storedTask -> assertUntilCommunityUserStateIs(state -> {

          if (state.attributes == null && state.attributes.containsKey("incentives")) {

            return false;

          } else {

            final var incentives = state.attributes.getJsonObject("incentives");
            final var questions = incentives.getLong("Questions", 0l);
            final var answers = incentives.getLong("Answers", 0l);
            final var answersAccepted = incentives.getLong("AnswersAccepted", 0l);
            return questions == 1l && answers == 0l && answersAccepted == 0l;

          }

        }, HardCodedQuestionAndAnswersProtocolIT.task.communityId, storedTask.requesterId, vertx, testContext));

    testContext.assertComplete(future).onSuccess(state -> testContext.completeNow());
  }

  /**
   * Check that an user answer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(4)
  public void shouldAnswer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.users.get(1).id;
    taskTransaction.label = "answerTransaction";
    final var answer = "Response to the question";
    taskTransaction.attributes = new JsonObject().put("answer", answer);
    final var newState = new State();
    newState.attributes = new JsonObject().put("incentives", new JsonObject().put("Answers", 3));
    final var future = WeNetInteractionProtocolEngine.createProxy(vertx)
        .mergeCommunityUserState(HardCodedQuestionAndAnswersProtocolIT.task.communityId, taskTransaction.actioneerId,
            newState)
        .compose(state -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction))
        .compose(done -> waitUntilCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

          for (var i = 0; i < callbacks.size(); i++) {

            final var msg = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
            if (msg != null && "AnsweredQuestionMessage".equals(msg.label)
                && HardCodedQuestionAndAnswersProtocolIT.task.id.equals(msg.attributes.getString("taskId"))
                && msg.attributes.getString("transactionId", null) != null
                && taskTransaction.actioneerId.equals(msg.attributes.getString("userId"))) {

              return true;

            }
          }
          return false;

        }, vertx, testContext))
        .compose(msg -> WeNetServiceSimulator.createProxy(vertx)
            .deleteCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, updatedTask -> {

          return updatedTask.transactions != null && updatedTask.transactions.size() == 2
              && updatedTask.transactions.get(1).messages != null
              && updatedTask.transactions.get(1).messages.size() == 1;

        }, vertx, testContext)).compose(task -> {

          HardCodedQuestionAndAnswersProtocolIT.task = task;
          testContext.verify(() -> {
            assertThat(task.transactions.get(1).id)
                .isEqualTo(task.transactions.get(1).messages.get(0).attributes.getString("transactionId", null));
          });
          return Future.succeededFuture(task);

        }).compose(task -> assertUntilCommunityUserStateIs(state -> {

          if (state.attributes == null) {

            return false;

          } else {

            final var answers = state.attributes.getJsonObject("incentives", new JsonObject()).getLong("Answers", 0l);
            return answers == 4l;

          }

        }, HardCodedQuestionAndAnswersProtocolIT.task.communityId, taskTransaction.actioneerId, vertx, testContext));
    testContext.assertComplete(future).onSuccess(state -> testContext.completeNow());

  }

  /**
   * Check that an user decline to answer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(5)
  public void shouldNoAnswer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.users
        .get(HardCodedQuestionAndAnswersProtocolIT.users.size() - 1).id;
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "notAnswerTransaction";
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 3
              && "notAnswerTransaction".equals(task.transactions.get(2).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardCodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(2).messages).isNull();
      testContext.completeNow();

    }));

  }

  /**
   * Check that ask some more users.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  public void shouldAskSomeMoreUsers(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "moreAnswerTransaction";
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 4
              && "moreAnswerTransaction".equals(task.transactions.get(3).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardCodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(3).messages).isNull();
      testContext.completeNow();

    }));

  }

  /**
   * Check that report question.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(7)
  public void shouldReportQuestion(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.users.get(2).id;
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "reportQuestionTransaction";
    taskTransaction.attributes = new JsonObject().put("reason", "Reason msg").put("comment", "Comment msg");
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 5
              && "reportQuestionTransaction".equals(task.transactions.get(4).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardCodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(4).messages).isNull();
      testContext.completeNow();

    }));

  }

  /**
   * Check that report answer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(8)
  public void shouldReportAnswer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "reportAnswerTransaction";
    taskTransaction.attributes = new JsonObject()
        .put("transactionId", HardCodedQuestionAndAnswersProtocolIT.task.transactions.get(1).id)
        .put("reason", "Reason msg").put("comment", "Comment msg");
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 6
              && "reportAnswerTransaction".equals(task.transactions.get(5).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardCodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(5).messages).isNull();
      testContext.completeNow();

    }));

  }

  /**
   * Check that pick the best answer.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(9)
  public void shouldBestAnswer(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.task != null;
    assert HardCodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardCodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardCodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "bestAnswerTransaction";
    taskTransaction.attributes = new JsonObject()
        .put("transactionId", HardCodedQuestionAndAnswersProtocolIT.task.transactions.get(1).id)
        .put("reason", "Reason msg");
    final var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 7
              && "bestAnswerTransaction".equals(task.transactions.get(6).label);

        }, vertx, testContext))
        .compose(done -> waitUntilCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

          for (var i = 0; i < callbacks.size(); i++) {

            final var notification = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
            if (notification != null && "AnsweredPickedMessage".equals(notification.label)
                && HardCodedQuestionAndAnswersProtocolIT.task.transactions.get(1).actioneerId
                    .equals(notification.receiverId)) {

              return true;

            }
          }

          return false;

        }, vertx, testContext))
        .compose(msg -> WeNetServiceSimulator.createProxy(vertx)
            .deleteCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, task -> {

          if (task.closeTs != null && task.transactions != null && task.transactions.size() == 7
              && "bestAnswerTransaction".equals(task.transactions.get(6).label)) {

            final var transaction = task.transactions.get(6);
            if ("bestAnswerTransaction".equals(transaction.label)) {

              if (transaction.messages != null && transaction.messages.size() == 1) {

                final var notification = transaction.messages.get(0);
                return "AnsweredPickedMessage".equals(notification.label)
                    && HardCodedQuestionAndAnswersProtocolIT.task.transactions.get(1).actioneerId
                        .equals(notification.receiverId);
              }
            }
          }

          return false;

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> {

      HardCodedQuestionAndAnswersProtocolIT.task = task;
      testContext.completeNow();

    });

  }

  /**
   * Check that create a second task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(10)
  public void shouldCreateSecondTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardCodedQuestionAndAnswersProtocolIT.users != null;
    assert HardCodedQuestionAndAnswersProtocolIT.app != null;

    final var task = new Task();
    task.appId = HardCodedQuestionAndAnswersProtocolIT.app.appId;
    task.attributes = new JsonObject().put("kindOfAnswerer", "anyone");
    task.taskTypeId = WeNetTaskManager.QUESTION_AND_ANSWER_TASK_TYPE_ID;
    task.goal = new HumanDescription();
    task.goal.name = "Test create task";
    task.requesterId = HardCodedQuestionAndAnswersProtocolIT.users.get(0).id;
    final var newState = new State();
    newState.attributes = new JsonObject().put("incentives", new JsonObject().put("Answers", 3));
    final var future = WeNetTaskManager.createProxy(vertx).createTask(task).compose(createdTask -> {
      HardCodedQuestionAndAnswersProtocolIT.task = createdTask;
      return waitUntilCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

        final Set<String> ids = new HashSet<>();
        for (final WeNetUserProfile profile : HardCodedQuestionAndAnswersProtocolIT.users) {

          ids.add(profile.id);
        }
        ids.remove(createdTask.requesterId);

        for (var i = 0; i < callbacks.size(); i++) {

          final var msg = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
          if (msg != null && "QuestionToAnswerMessage".equals(msg.label)
              && createdTask.id.equals(msg.attributes.getString("taskId"))
              && createdTask.goal.name.equals(msg.attributes.getString("question"))
              && createdTask.requesterId.equals(msg.attributes.getString("userId"))) {

            ids.remove(msg.receiverId);

          }
        }
        return ids.isEmpty();

      }, vertx, testContext);

    }).compose(msg -> WeNetServiceSimulator.createProxy(vertx)
        .deleteCallbacks(HardCodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardCodedQuestionAndAnswersProtocolIT.task.id, updatedTask -> {

          return updatedTask.transactions != null && updatedTask.transactions.size() == 1
              && updatedTask.transactions.get(0).messages != null && updatedTask.transactions.get(0).messages
                  .size() == HardCodedQuestionAndAnswersProtocolIT.users.size() - 1;

        }, vertx, testContext)).compose(storedTask -> {

          HardCodedQuestionAndAnswersProtocolIT.task = storedTask;
          testContext.verify(() -> {

            assertThat(storedTask.transactions).isNotEmpty();
            final var transaction = storedTask.transactions.get(0);
            assertThat(transaction.messages).isNotEmpty();
            final var messages = new ArrayList<>(transaction.messages);
            for (final WeNetUserProfile profile : HardCodedQuestionAndAnswersProtocolIT.users) {

              final var iter = messages.iterator();
              while (iter.hasNext()) {

                final var message = iter.next();
                if (profile.id.equals(message.receiverId)) {

                  iter.remove();
                  break;
                }

              }
            }

            assertThat(messages).isEmpty();
          });

          return Future.succeededFuture(storedTask);

        }).compose(storedTask -> assertUntilCommunityUserStateIs(state -> {

          if (state.attributes == null && state.attributes.containsKey("incentives")) {

            return false;

          } else {

            final var incentives = state.attributes.getJsonObject("incentives");
            final var questions = incentives.getLong("Questions", 0l);
            final var answers = incentives.getLong("Answers", 0l);
            final var answersAccepted = incentives.getLong("AnswersAccepted", 0l);
            return questions == 2l && answers == 0l && answersAccepted == 0l;

          }

        }, HardCodedQuestionAndAnswersProtocolIT.task.communityId, storedTask.requesterId, vertx, testContext));

    testContext.assertComplete(future).onSuccess(state -> testContext.completeNow());
  }

}
