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

import static eu.internetofus.common.components.profile_manager.WeNetProfileManagers.createUsers;
import static eu.internetofus.common.components.service.WeNetServiceSimulators.createApp;
import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;
import static eu.internetofus.common.components.task_manager.WeNetTaskManagers.waitUntilTask;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.HumanDescription;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.Message;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskTransaction;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
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
public class HardcodedQuestionAndAnswersProtocolIT {

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
      HardcodedQuestionAndAnswersProtocolIT.users = users;
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

    assert HardcodedQuestionAndAnswersProtocolIT.users != null;
    testContext.assertComplete(createApp(HardcodedQuestionAndAnswersProtocolIT.users, vertx, testContext))
        .onSuccess(app -> {
          HardcodedQuestionAndAnswersProtocolIT.app = app;
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

    assert HardcodedQuestionAndAnswersProtocolIT.users != null;
    assert HardcodedQuestionAndAnswersProtocolIT.app != null;

    final var task = new Task();
    task.appId = HardcodedQuestionAndAnswersProtocolIT.app.appId;
    task.attributes = new JsonObject().put("kindOfAnswerer", "anyone");
    task.taskTypeId = WeNetTaskManager.QUESTION_AND_ANSWER_TASK_TYPE_ID;
    task.goal = new HumanDescription();
    task.goal.name = "Test create task";
    task.requesterId = HardcodedQuestionAndAnswersProtocolIT.users.get(0).id;
    var future = WeNetTaskManager.createProxy(vertx).createTask(task).compose(createdTask -> {

      HardcodedQuestionAndAnswersProtocolIT.task = createdTask;
      return waitUntilCallbacks(HardcodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

        final Set<String> ids = new HashSet<>();
        for (final WeNetUserProfile profile : HardcodedQuestionAndAnswersProtocolIT.users) {

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
        .deleteCallbacks(HardcodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, updatedTask -> {

          return updatedTask.transactions != null && updatedTask.transactions.size() == 1
              && updatedTask.transactions.get(0).messages != null && updatedTask.transactions.get(0).messages
                  .size() == HardcodedQuestionAndAnswersProtocolIT.users.size() - 1;

        }, vertx, testContext));

    testContext.assertComplete(future).onSuccess(storedTask -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = storedTask;
      assertThat(storedTask.transactions).isNotEmpty();
      var transaction = storedTask.transactions.get(0);
      assertThat(transaction.messages).isNotEmpty();
      var messages = new ArrayList<>(transaction.messages);
      for (final WeNetUserProfile profile : HardcodedQuestionAndAnswersProtocolIT.users) {

        var iter = messages.iterator();
        while (iter.hasNext()) {

          var message = iter.next();
          if (profile.id.equals(message.receiverId)) {

            iter.remove();
            break;
          }

        }
      }

      assertThat(messages).isEmpty();
      testContext.completeNow();
    }));
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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.users.get(1).id;
    taskTransaction.label = "answerTransaction";
    var answer = "Response to the question";
    taskTransaction.attributes = new JsonObject().put("answer", answer);
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilCallbacks(HardcodedQuestionAndAnswersProtocolIT.app.appId, callbacks -> {

          for (var i = 0; i < callbacks.size(); i++) {

            final var msg = Model.fromJsonObject(callbacks.getJsonObject(i), Message.class);
            if (msg != null && "AnsweredQuestionMessage".equals(msg.label)
                && HardcodedQuestionAndAnswersProtocolIT.task.id.equals(msg.attributes.getString("taskId"))
                && msg.attributes.getString("transactionId", null) != null
                && taskTransaction.actioneerId.equals(msg.attributes.getString("userId"))) {

              return true;

            }
          }
          return false;

        }, vertx, testContext))
        .compose(msg -> WeNetServiceSimulator.createProxy(vertx)
            .deleteCallbacks(HardcodedQuestionAndAnswersProtocolIT.app.appId))
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, updatedTask -> {

          return updatedTask.transactions != null && updatedTask.transactions.size() == 2
              && updatedTask.transactions.get(1).messages != null
              && updatedTask.transactions.get(1).messages.size() == 1;

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(1).id)
          .isEqualTo(task.transactions.get(1).messages.get(0).attributes.getString("transactionId", null));
      testContext.completeNow();

    }));

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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.users
        .get(HardcodedQuestionAndAnswersProtocolIT.users.size() - 1).id;
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "notAnswerTransaction";
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 3
              && "notAnswerTransaction".equals(task.transactions.get(2).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "moreAnswerTransaction";
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 4
              && "moreAnswerTransaction".equals(task.transactions.get(3).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.users.get(2).id;
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "reportQuestionTransaction";
    taskTransaction.attributes = new JsonObject().put("reason", "Reason msg").put("comment", "Comment msg");
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 5
              && "reportQuestionTransaction".equals(task.transactions.get(4).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "reportAnswerTransaction";
    taskTransaction.attributes = new JsonObject()
        .put("transactionId", HardcodedQuestionAndAnswersProtocolIT.task.transactions.get(1).id)
        .put("reason", "Reason msg").put("comment", "Comment msg");
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 6
              && "reportAnswerTransaction".equals(task.transactions.get(5).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
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

    assert HardcodedQuestionAndAnswersProtocolIT.task != null;
    assert HardcodedQuestionAndAnswersProtocolIT.users != null;

    final var taskTransaction = new TaskTransaction();
    taskTransaction.actioneerId = HardcodedQuestionAndAnswersProtocolIT.task.requesterId;
    taskTransaction.taskId = HardcodedQuestionAndAnswersProtocolIT.task.id;
    taskTransaction.label = "bestAnswerTransaction";
    taskTransaction.attributes = new JsonObject()
        .put("transactionId", HardcodedQuestionAndAnswersProtocolIT.task.transactions.get(1).id)
        .put("reason", "Reason msg");
    var future = WeNetTaskManager.createProxy(vertx).doTaskTransaction(taskTransaction)
        .compose(done -> waitUntilTask(HardcodedQuestionAndAnswersProtocolIT.task.id, task -> {

          return task.transactions != null && task.transactions.size() == 7
              && "bestAnswerTransaction".equals(task.transactions.get(6).label);

        }, vertx, testContext));
    testContext.assertComplete(future).onSuccess(task -> testContext.verify(() -> {

      HardcodedQuestionAndAnswersProtocolIT.task = task;
      assertThat(task.transactions.get(6).messages).isNull();
      testContext.completeNow();

    }));

  }

}
