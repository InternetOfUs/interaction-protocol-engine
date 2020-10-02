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
import eu.internetofus.common.components.interaction_protocol_engine.Message;
import eu.internetofus.common.components.interaction_protocol_engine.Message.Type;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.TaskProposalNotification;
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
   * Create the users that will be used on the tests.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(1)
  public void shouldCreateUsers(final Vertx vertx, final VertxTestContext testContext) {

    createUsers(MAX_USERS, vertx, testContext).onComplete(testContext.succeeding(users -> {
      HardcodedProtocolIT.users = users;
      testContext.completeNow();
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

    assert HardcodedProtocolIT.users != null;
    createApp(HardcodedProtocolIT.users, vertx, testContext).onComplete(testContext.succeeding(app -> {
      HardcodedProtocolIT.app = app;
      testContext.completeNow();
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

    assert HardcodedProtocolIT.users != null;
    assert HardcodedProtocolIT.app != null;
    StoreServices.storeTaskTypeExample(1, vertx, testContext, testContext.succeeding(taskType -> {

      HardcodedProtocolIT.taskType = taskType;
      final var task = new Task();
      task.appId = HardcodedProtocolIT.app.appId;
      task.deadlineTs = TimeManager.now() + 120;
      task.startTs = task.deadlineTs + 30;
      task.endTs = task.startTs + 300;
      task.taskTypeId = taskType.id;
      task.goal = new TaskGoal();
      task.goal.name = "Test create task";
      task.requesterId = HardcodedProtocolIT.users.get(0).id;
      WeNetTaskManager.createProxy(vertx).createTask(task, testContext.succeeding(createdTask -> {

        HardcodedProtocolIT.task = createdTask;
        waitUntilCallbacks(HardcodedProtocolIT.app.appId, callbacks -> {

          final Set<String> ids = new HashSet<>();
          for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

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
          WeNetServiceSimulator.createProxy(vertx).deleteCallbacks(HardcodedProtocolIT.app.appId, testContext.succeeding(removed -> {
            WeNetTaskManager.createProxy(vertx).retrieveTask(HardcodedProtocolIT.task.id, testContext.succeeding(storedTask -> testContext.verify(() -> {

              assertThat(storedTask.attributes).isNotNull().isInstanceOf(JsonObject.class);
              final var attributes = storedTask.attributes;
              final var unanswered = attributes.getJsonArray("unanswered");
              assertThat(unanswered).isNotNull().doesNotContain(createdTask.requesterId);
              for (final WeNetUserProfile profile : HardcodedProtocolIT.users) {

                if (!profile.id.equals(storedTask.requesterId)) {

                  assertThat(unanswered).contains(profile.id);

                }
              }
              HardcodedProtocolIT.task = storedTask;
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
  public void shouldDeclineTask(final Vertx vertx, final VertxTestContext testContext) {

    assert HardcodedProtocolIT.task != null;
    assert HardcodedProtocolIT.users != null;
    assert HardcodedProtocolIT.app != null;

    final var message = new Message();
    message.type = Type.SWI_PROLOG;
    message.appId = HardcodedProtocolIT.app.appId;
    message.taskId = HardcodedProtocolIT.task.id;
    message.senderId = HardcodedProtocolIT.users.get(1).id;
    message.particle = "declined";
    message.content = new JsonObject().put("task",this.toArray(HardcodedProtocolIT.task));
    WeNetInteractionProtocolEngine.createProxy(vertx).sendMessage(message, testContext.succeeding(sentMessage -> {

      waitUntilTask(HardcodedProtocolIT.task.id, task -> {

        return task.attributes instanceof JsonObject && task.attributes.getJsonArray("declined", new JsonArray()).contains(message.senderId);

      }, vertx, testContext).onComplete(testContext.succeeding(task -> testContext.verify(() -> {

        assertThat(task.attributes).isNotNull().isInstanceOf(JsonObject.class);
        final var attributes = task.attributes;
        final var unanswered = attributes.getJsonArray("unanswered");
        assertThat(unanswered).isNotNull().doesNotContain(task.requesterId, message.senderId);
        final var declined = attributes.getJsonArray("declined");
        assertThat(declined).isNotNull().hasSize(1).contains(message.senderId);
        testContext.completeNow();

      })));

    }));

  }

  /**
   * Convert a task to the List that can be used on the norm engine.
   *
   * @param task to convert.
   *
   * @return the array that represents the task.
   */
  private JsonArray toArray(final Task task) {

    return new JsonArray().add(task.id).add(task.goal.name).add(new JsonArray().add(task.goal.description)).add(task.deadlineTs).add(task.attributes.getJsonArray("unanswered", new JsonArray()))
        .add(task.attributes.getJsonArray("declined", new JsonArray())).add(task.attributes.getJsonArray("volunteers", new JsonArray())).add(task.attributes.getJsonArray("accepted", new JsonArray()));

  }

}
