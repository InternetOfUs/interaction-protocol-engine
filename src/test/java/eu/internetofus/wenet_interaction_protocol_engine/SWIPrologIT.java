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
import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;
import static eu.internetofus.common.components.task_manager.WeNetTaskManagers.waitUntilTask;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.CommunityProfile;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TaskProposalNotification;
import eu.internetofus.common.components.service.WeNetService;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.task_manager.Task;
import eu.internetofus.common.components.task_manager.TaskGoal;
import eu.internetofus.common.components.task_manager.TaskType;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;

/**
 * Check the integration for the SWIProlog.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
@TestMethodOrder(OrderAnnotation.class)
public class SWIPrologIT {

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
   * The task type that will involved on the test.
   */
  protected static TaskType taskType;

  /**
   * The task that will involved on the test.
   */
  protected static Task task;

  /**
   * The community that will involved on the test.
   */
  protected static CommunityProfile community;

  /**
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    StoreServices.storeProfileExample(1, vertx, testContext, testContext.succeeding(me -> {

      createUsers(MAX_USERS, vertx, testContext).onComplete(testContext.succeeding(users -> {
        SWIPrologIT.users = users;
        SWIPrologIT.users.add(0, me);
        testContext.completeNow();
      }));

    }));
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

    assert SWIPrologIT.users != null;
    StoreServices.storeCommunityExample(1, vertx, testContext, testContext.succeeding(community -> {

      SWIPrologIT.community = community;
      WeNetService.createProxy(vertx).retrieveApp(community.appId, testContext.succeeding(app -> {
        SWIPrologIT.app = app;
        final var appUsers = new JsonArray();
        for (final WeNetUserProfile profile : users) {

          appUsers.add(profile.id);
        }
        WeNetServiceSimulator.createProxy(vertx).addUsers(app.appId, appUsers, testContext.succeeding(added -> testContext.completeNow()));

      }));

    }));

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

    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;
    StoreServices.storeTaskTypeExample(1, vertx, testContext, testContext.succeeding(taskType -> {

      SWIPrologIT.taskType = taskType;
      final var task = new Task();
      task.appId = SWIPrologIT.app.appId;
      // task.communityId = SWIPrologIT.community.id;
      task.deadlineTs = TimeManager.now() + 120;
      task.startTs = task.deadlineTs + 30;
      task.endTs = task.startTs + 300;
      task.taskTypeId = taskType.id;
      task.goal = new TaskGoal();
      task.goal.name = "Test create task";
      task.requesterId = SWIPrologIT.users.get(0).id;
      WeNetTaskManager.createProxy(vertx).createTask(task, testContext.succeeding(createdTask -> {

        SWIPrologIT.task = createdTask;
        waitUntilCallbacks(SWIPrologIT.app.appId, callbacks -> {

          final Set<String> ids = new HashSet<>();
          for (final WeNetUserProfile profile : SWIPrologIT.users) {

            ids.add(profile.id);
          }
          ids.remove(createdTask.requesterId);

          for (var i = 0; i < callbacks.size(); i++) {

            final var proposal = Model.fromJsonObject(callbacks.getJsonObject(i), TaskProposalNotification.class);
            if (proposal != null && createdTask.id.equals(proposal.taskId)) {

              ids.remove(proposal.recipientId);

            }
          }
          return ids.isEmpty();

        }, vertx, testContext).onComplete(testContext.succeeding(msg -> {
          WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(SWIPrologIT.app.appId, testContext.succeeding(removed -> {
            WeNetTaskManager.createProxy(vertx).retrieveTask(SWIPrologIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

              assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
              final var attributes = storedTask.attributes;
              final var unanswered = attributes.getJsonArray("unanswered");
              assertThat(unanswered).isNotNull().doesNotContain(createdTask.requesterId);
              for (final WeNetUserProfile profile : SWIPrologIT.users) {

                if (!profile.id.equals(storedTask.requesterId)) {

                  assertThat(unanswered).contains(profile.id);

                }
              }
              SWIPrologIT.task = storedTask;
              testContext.completeNow();
            })));
          }));
        }));

      }));
    }));
  }

  /**
   * Check that an user decline to do a task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(4)
  public void shouldCreateTaskWithNorms(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.task != null;
    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;

    final var message = new ProtocolMessage();
    message.appId = SWIPrologIT.app.appId;
    message.communityId = SWIPrologIT.community.id;
    message.taskId = SWIPrologIT.task.id;
    message.sender = new ProtocolAddress();
    message.sender.component = Component.TASK_MANAGER;
    message.sender.userId = SWIPrologIT.users.get(0).id;
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    message.receiver.userId = task.requesterId;
    message.particle = "createTask";
    message.content = new JsonObject();
    WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message, testContext.succeeding(sentMessage -> {

      waitUntilCallbacks(SWIPrologIT.app.appId, callbacks -> {

        final Set<String> ids = new HashSet<>();
        for (final WeNetUserProfile profile : SWIPrologIT.users) {

          ids.add(profile.id);
        }
        ids.remove(SWIPrologIT.task.requesterId);

        for (var i = 0; i < callbacks.size(); i++) {

          final var proposal = Model.fromJsonObject(callbacks.getJsonObject(i), TaskProposalNotification.class);
          if (proposal != null && SWIPrologIT.task.id.equals(proposal.taskId)) {

            ids.remove(proposal.recipientId);

          }
        }
        return ids.isEmpty();

      }, vertx, testContext).onComplete(testContext.succeeding(msg -> {
        WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(SWIPrologIT.app.appId, testContext.succeeding(removed -> {
          WeNetTaskManager.createProxy(vertx).retrieveTask(SWIPrologIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

            assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
            final var attributes = storedTask.attributes;
            final var unanswered = attributes.getJsonArray("unanswered");
            assertThat(unanswered).isNotNull().doesNotContain(SWIPrologIT.task.requesterId);
            for (final WeNetUserProfile profile : SWIPrologIT.users) {

              if (!profile.id.equals(storedTask.requesterId)) {

                assertThat(unanswered).contains(profile.id);

              }
            }
            SWIPrologIT.task = storedTask;
            testContext.completeNow();
          })));
        }));
      }));

    }));

  }

  /**
   * Check that an user decline to do a task.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  @Test
  @Order(5)
  public void shouldDeclineTask(final Vertx vertx, final VertxTestContext testContext) {

    assert SWIPrologIT.task != null;
    assert SWIPrologIT.users != null;
    assert SWIPrologIT.app != null;

    final var message = new ProtocolMessage();
    message.appId = SWIPrologIT.app.appId;
    message.communityId = SWIPrologIT.community.id;
    message.taskId = SWIPrologIT.task.id;
    final var userId = SWIPrologIT.users.get(1).id;
    message.sender = new ProtocolAddress();
    message.sender.component = Component.TASK_MANAGER;
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    message.receiver.userId = userId;
    message.particle = "declined";
    message.content = new JsonObject();
    WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message, testContext.succeeding(sentMessage -> {

      waitUntilTask(SWIPrologIT.task.id, task -> {

        return task.attributes instanceof JsonObject && task.attributes.getJsonArray("declined", new JsonArray()).contains(userId);

      }, vertx, testContext).onComplete(testContext.succeeding(task -> testContext.verify(() -> {

        assertThat(task.attributes).isNotNull().isInstanceOf(JsonObject.class);
        final var attributes = task.attributes;
        final var unanswered = attributes.getJsonArray("unanswered");
        assertThat(unanswered).isNotNull().doesNotContain(task.requesterId, userId);
        final var declined = attributes.getJsonArray("declined");
        assertThat(declined).isNotNull().hasSize(1).contains(userId);
        testContext.completeNow();

      })));

    }));

  }

}
